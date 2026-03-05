# LANDMARKS

Updated: 2026-03-05
Context: compiler/bootstrap/stage2-stability

[LM-1|goal]: `stage1 -> stage2` bootstrap is reproducibly buildable in `--release` on current branch (how: `scripts/build_stage1_original_release.sh` + `scripts/build_stage2_release.sh`) {F/G/R: 0.9/0.6/0.9} [verified]

[LM-2|pattern]: stage2-only failure class concentrates around generic container paths (`Hash/Set`, clear/upsert, ivar-heavy backend paths) (how: repeated LLDB tops in `Hash(... )#upsert`, `LLVMIRGenerator#reset_value_names`) {F/G/R: 0.8/0.7/0.8} [working]

[LM-3|repro]: focused stage2 repro exists independent of full bootstrap source: `regression_tests/stage2_container_clear_index_oob_repro.sh` fails on stage2 with `Index out of bounds` and passes on stage1 control {F/G/R: 0.9/0.7/0.9} [verified]

[LM-4|perf]: recent release timings show stage2 roughly in same range or slightly faster than stage1 for full compiler compile in this local setup (e.g. `stage1 439.48s`, `stage2 427.27s`; clean-cache stage2 run `330.28s`) {F/G/R: 0.9/0.5/0.7} [working]

[LM-5|boundary]: `stage_stats_output_repro.sh` still fails on stage2 (`Index out of bounds`) and stage2->stage3 remains unstable {F/G/R: 0.9/0.6/0.9} [verified]

Contradiction ledger
- [LM-C1|refute]: broad `reset_value_names` reinit experiment (replace many `clear` with fresh container allocations) did not produce robust stabilization; it shifted crash boundaries and was rejected.
- [LM-C2|refute]: cache-only explanation is insufficient: fresh stage2 cache can change performance and some stack tops, but core stage2 failure signatures remain.

Current hypothesis
- Root-cause cluster is in monomorphization/lowering/ABI for generic container-heavy code paths (especially ivar-dense backend methods), not in one specific callsite.
