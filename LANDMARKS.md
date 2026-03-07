# LANDMARKS

Updated: 2026-03-07
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

[LM-26|boundary]: after the parser fix is rebuilt in a clean worktree (`94598345`), the active stage2 blocker shifts from `from_stdio` leak to total post-`opt` collapse: raw stage2 `--release --no-link` IR still contains `main`, `__crystal_main`, and `main_user_code`, but `opt` reduces the module to near-empty bitcode (`1.4K`) and an empty-symbol object (`336B`) {F/G/R: 0.95/0.8/0.95} [verified]

[LM-27|repro]: focused oracle `regression_tests/stage2_opt_empty_module_repro.sh` reproduces the new clean blocker directly from raw stage2 IR: both `O3` and `O0` `opt` runs erase `main`, `__crystal_main`, and `main_user_code`, leaving zero `define` lines in disassembled output and zero exported symbols in the object {F/G/R: 0.95/0.8/0.95} [verified]

[LM-28|root-cause]: the clean post-`opt` empty-module blocker was introduced by CLI entry-guard rewriting, not LLVM `opt`: `apply_llvm_entry_opt_guard!` read `.ll` via `File.each_line(ll_file)` with default `chomp=true`, dropped all `\n`, and flattened the file into one giant comment line beginning with `; ModuleID = ...`; after changing it to `File.each_line(ll_file, chomp: false)`, raw stage2 IR again contains millions of newline bytes, `stage2_opt_empty_module_repro.sh` stops reproducing, and post-`opt` artifacts return to normal size/symbol counts {F/G/R: 0.98/0.85/0.98} [verified]

[LM-29|perf]: on warm release caches after the entry-guard newline fix, clean rebuild timings are `stage1 --release = 410.34s` and `stage2 --release = 216.97s`, for a current observed `~1.89x` speedup of stage2 over stage1 on this host {F/G/R: 0.95/0.5/0.95} [verified]

[LM-30|boundary]: after the newline fix removes the false empty-module blocker, `stage2 -> stage3` no longer hangs in the previous self-loop class; it now fails fast on `src/crystal_v2.cr --release` with non-debug surface `error: Index out of bounds` (`real 1.26`), while `CRYSTAL2_STAGE2_DEBUG=1` turns the same attempt into `rc=139` {F/G/R: 0.95/0.75/0.95} [verified]

[LM-31|boundary]: the old minimal BlockId-clear signature drifted again on the new stage2: `stage2_emit_function_blockid_clear_repro.sh` no longer reproduces, but its LLDB trace still shows a stage2-only crash in `LLVMIRGenerator#generate` immediately after `[LLVM] total MIR functions: 1`, so the active minimal codegen crash boundary is now broader/earlier than the previous `Set(HIR::BlockId)#clear` path {F/G/R: 0.9/0.7/0.9} [verified]

[LM-32|root-cause]: non-inline methods with `&block` value parameters are currently mislowered as block-overloads without a usable runtime proc value: tiny repro `regression_tests/proc_block_value_param_store_repro.sh` prints `before=false` / `after=false`, IR lowers `EventEmitter#on_event` into a getter returning `@on_event`, and standalone def lowering excludes block params from `param_infos` / `func.add_param` while inline lowering has a special `&block -> lower_block_to_proc` path {F/G/R: 0.95/0.75/0.95} [verified]

[LM-33|root-cause]: capturing block->Proc materialization loses writes to outer locals: `regression_tests/proc_block_capture_write_repro.sh` prints an empty line while non-capturing control prints `ok`; IR shows `call ptr @__crystal_block_proc_0(ptr @.str.50)`, `define ptr @__crystal_block_proc_0(ptr %v) { ret ptr %v }`, and the caller later still prints the original empty-string constant, so `result = v` does not propagate back to outer state {F/G/R: 0.95/0.7/0.95} [verified]

[LM-34|boundary]: `String.build` failure is broader than the earlier "missing block CFG setup" hypothesis: tiny repro prints `x=0` and empty result, and no-link IR emits only `String::Builder.new` + `to_s` with no block-body marker (`SBMARK`) or `x = 1` side effect at all, so the body disappears before/within intrinsic lowering {F/G/R: 0.9/0.7/0.95} [verified]

[LM-35|root-cause]: enum instance-method failure is real, but the strongest current evidence does not support "registered on Int32" as the initial bug site: registration code passes `enum_name`, while tiny repro no-link IR still lowers the call site to `Int32$Htag` dead stub and emits no `OmniColor$Htag`, so enum identity is being lost later in resolution/lowering {F/G/R: 0.9/0.7/0.95} [verified]

[LM-36|root-cause]: `RC-2A` was not a def-lowering ABI omission; the explicit block target already resolved to `EventEmitter#on_event$block`, but `lower_call` ran `ensure_accessor_method` before lazy lowering and treated "body not emitted yet" as "method missing", hijacking the call into a synthetic ivar getter `EventEmitter#on_event` because the receiver also had `@on_event` {F/G/R: 0.95/0.8/0.95} [verified]

[LM-37|fix]: guarding synthetic accessor fallback on `missing_impl` with explicit-target presence (`@function_defs/@function_types` for `primary_mangled_name` / `mangled_method_name`) removes the `RC-2A` hijack without regressing accessor-backed calls {F/G/R: 0.9/0.7/0.9} [verified]

[LM-38|verify]: after the accessor-guard fix, `regression_tests/proc_block_value_param_store_repro.sh ./bin/crystal_v2` no longer reproduces (`before=false`, `after=true`), `regression_tests/complex/test_nilable_proc.cr` prints `nilable_proc_ok`, `regression_tests/proc_block_capture_write_repro.sh ./bin/crystal_v2` still reproduces `RC-2B`, a `Range#begin` accessor probe still prints `1`, and `regression_tests/run_all.sh ./bin/crystal_v2` finishes `65 passed, 0 failed` in `775.91s` {F/G/R: 0.95/0.6/0.95} [verified]

[LM-39|root-cause]: `RC-2B` was not a parser/block-AST loss and not a generic closure-cell failure; the source block still parses as `AssignNode(result = v)`, direct `ProcLiteral` capture/writeback already works, and the failing edge is inline `&block` param binding: `inline_yield_function` clears `ctx` to callee-only locals before calling `lower_block_to_proc`, restoring only lexical `self`, so caller locals like `result` are absent from `ctx.save_locals` and the block proc lowers `result = v` as a new dead local {F/G/R: 0.95/0.8/0.95} [verified]

[LM-40|fix]: temporarily restoring the full `caller_locals` snapshot while materializing an inline `&block` Proc, then restoring the inline callee locals afterward, makes `lower_block_to_proc` capture outer locals correctly without widening the scope of the inline call itself {F/G/R: 0.9/0.7/0.9} [verified]

[LM-41|verify]: after the inline caller-locals restore fix, `regression_tests/proc_block_capture_write_repro.sh ./bin/crystal_v2` no longer reproduces (`stdout: ok`), `regression_tests/proc_block_value_param_store_repro.sh ./bin/crystal_v2` stays green, direct proc-literal capture control still prints `ok`, no-link LLVM for the tiny repro shows caller seed/store/load through `@__closure__classvar____closure_cell_0`, and `regression_tests/run_all.sh ./bin/crystal_v2` finishes `65 passed, 0 failed` in `746.29s` {F/G/R: 0.95/0.65/0.95} [verified]

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
- [LM-C11|refute]: the `_main` link failure after the parser fix is not explained by missing entry generation; clean raw stage2 `.ll` still contains `main`, `__crystal_main`, and `main_user_code` before optimization.
- [LM-C12|refute]: the current clean post-parser-fix collapse is not limited to the earlier `tailrecurse` `O3` signature; the module also empties under `opt -O0`, so the new blocker is broader than a single aggressive optimization pass.
- [LM-C13|refute]: the clean empty-module blocker is not a pure LLVM-`opt` semantic collapse; the decisive trigger was our own entry-guard rewrite path flattening `.ll` by dropping line terminators during `File.each_line` rewrite.
- [LM-C14|refute]: broad `RC-2 = union Proc#call bypass` is insufficient; direct non-capturing `block.call` already works, while separate `&block` value-ABI and block-capture-write bugs still reproduce before union dispatch becomes relevant.
- [LM-C15|refute]: the stronger `String.build` claim "only missing block CFG setup" is not yet verified; current IR evidence shows the block body vanishes entirely, which is broader than a single CFG bookkeeping omission.
- [LM-C16|refute]: "enum methods are registered on Int32" is unsupported by the current registration code; loss of enum identity is observed later at call resolution/lowering.
- [LM-C17|refute]: the earlier `RC-2A` wording "non-inline `&block` params are omitted from def lowering / runtime ABI" is false on the current branch; the explicit block target is registered and lowered, but call lowering was switching to a synthetic accessor before the explicit target body was emitted.
- [LM-C18|refute]: `RC-2B` is not explained by parser/block-AST loss or by generally broken closure writeback; the block AST still contains `AssignNode(result = v)`, ordinary `ProcLiteral` capture/writeback works, and the actual failure site is inline `&block` param lowering after `caller_locals` were stripped from `ctx`.

Current hypothesis
- The false empty-module blocker is removed by the entry-guard newline fix ([LM-28]), so the active work returns to genuine stage2 runtime/codegen instability. Current evidence points to a still-broad stage2-only failure family: `stage2_container_clear_index_oob_repro.sh` still reproduces `Index out of bounds`, `stage2 -> stage3` now fails fast instead of hanging ([LM-30]), and the smallest current codegen repro crashes inside `LLVMIRGenerator#generate` before the older BlockId-clear path ([LM-31]). The next useful work is to build a new focused oracle for that earlier generate-path crash and then root-cause the shared invariant violation behind the fast `Index out of bounds` / `EXC_BAD_ACCESS` behavior.
- Separate from the stage2 crash family, the proc/string/enum regression map is now narrower and more actionable: the `RC-2A` block-call hijack is fixed ([LM-36], [LM-37], [LM-38]), `RC-2B` block-capture-write loss is also fixed via caller-local restoration during inline `&block` materialization ([LM-39], [LM-40], [LM-41]), `String.build` still drops its block body but the earlier CFG-only explanation is not yet proven ([LM-34]), and enum method dispatch still loses type identity later than initial enum-method registration ([LM-35]).
