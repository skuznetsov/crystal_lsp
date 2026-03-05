# LANDMARKS

Updated: 2026-03-05
Context: compiler/bootstrap/stage2-stability

[LM-1|goal]: `stage1 -> stage2` bootstrap is reproducibly buildable in `--release` on current branch (how: `scripts/build_stage1_original_release.sh` + `scripts/build_stage2_release.sh`) {F/G/R: 0.9/0.6/0.9} [verified]

[LM-2|pattern]: stage2-only failure class is broader than container-specific triggers: minimal `trivial.cr` / `macro.cr` also fail on stage2 while stage1 passes (how: direct control matrix with `/tmp/stage1_rel_flags_fix` vs `/tmp/stage2_rel_flags_fix`) {F/G/R: 0.9/0.7/0.9} [verified]

[LM-3|repro]: focused stage2 repro exists independent of full bootstrap source: `regression_tests/stage2_container_clear_index_oob_repro.sh` (single `Hash(UInt32, String)` + `clear`) fails on stage2 with `Index out of bounds` and passes on stage1 control {F/G/R: 0.9/0.8/0.9} [verified]

[LM-4|perf]: release timings continue to show stage2 in the same range or faster than stage1 on this host, but with high run-to-run variance from cache/workspace state (examples: `stage1 439.48s -> stage2 427.27s`; latest full run `stage1 725.24s -> stage2 685.75s`) {F/G/R: 0.9/0.5/0.7} [working]

[LM-5|boundary]: `stage_stats_output_repro.sh` still fails on stage2 (`Index out of bounds`) and stage2->stage3 remains unstable {F/G/R: 0.9/0.6/0.9} [verified]

[LM-6|boundary]: updated `stage2_llvm_setup_pre_generate_segfault_repro.sh` signature remains stage2-only and catches current boundary drift (status `139`, `[MIR_SETUP] before lowering.new`, no `[LLVM_SETUP] generate(io|string) done`) {F/G/R: 0.9/0.7/0.9} [verified]

[LM-7|boundary]: with `LLVM_INIT` trace enabled, stage2 reaches `after generator.new` and `generate(io) start`, then crashes before `generate(io) done` on minimal macro repro {F/G/R: 0.9/0.7/0.9} [verified]

[LM-8|boundary]: with `LLVM_GEN` trace enabled, stage2 reaches `emit_function start __crystal_main` and then crashes inside `reset_value_names` clear-sequence before `emit_function done` {F/G/R: 0.9/0.7/0.9} [verified]

[LM-9|repro]: focused oracle `regression_tests/stage2_reset_value_names_fiberevent_clear_repro.sh` reproduces current stage2 crash class by combining runtime signal (`status=139`, `[MIR_SETUP] before lowering.new`, no `[LLVM_SETUP] generate(io) done`) with static disasm signal (`reset_value_names` contains `FiberEvent$Hclear`) {F/G/R: 0.9/0.8/0.9} [verified]

[LM-10|root-cause]: LLDB on `/tmp/stage2_dbg_reset_probe` confirms crash at `FiberEvent#clear` called from `LLVMIRGenerator#reset_value_names`, and disassembly shows two `FiberEvent$Hclear` targets embedded in reset clear-sequence (expected hash/array clears), indicating call-target/receiver-layout corruption rather than a single container API defect {F/G/R: 0.9/0.8/0.9} [verified]

[LM-11|stability]: latest release bootstrap chain still fails at stage2->stage3 despite successful stage1->stage2 build (`/tmp/stage2_rel_current`), with immediate `Index out of bounds` on stage3 attempt (`real 0.67`) {F/G/R: 0.9/0.7/0.9} [verified]

Contradiction ledger
- [LM-C1|refute]: broad `reset_value_names` reinit experiment (replace many `clear` with fresh container allocations) did not produce robust stabilization; it shifted crash boundaries and was rejected.
- [LM-C2|refute]: cache-only explanation is insufficient: fresh stage2 cache can change performance and some stack tops, but core stage2 failure signatures remain.
- [LM-C3|refute]: `Set`-specific root-cause hypothesis is insufficient; stage2 fails equally on trivial/no-container and `Hash/Set` micro-repros.
- [LM-C4|refute]: targeted `@alloc_types.clear -> reinit` workaround in `reset_value_names` was unstable and regressed crash boundary; rejected.
- [LM-C5|refute]: converting `@alloc_types/@alloc_element_types` from Hash to array-indexed storage did not provide robust stage2 stabilization; boundary remained unstable and change was reverted.

Current hypothesis
- Root-cause cluster is a broader stage2 self-host corruption class (monomorphization/lowering/runtime ABI interactions), now tightly localized to per-function reset/clear paths in LLVM emission (`reset_value_names`) during first function emission.
